;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* _sc/instrument
;;; NAME 
;;; instrument
;;;
;;; File:             instrument.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> instrument
;;;
;;; Version:          1.0
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
;;; $$ Last modified: 13:05:17 Sat Feb 26 2011 ICT
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

;;; The id slot stores the instrument name; the data slot is unused.

;;; The lowest and highest notes of the instrument can be given either in
;;; -written or -sounding form whereby if both are given, it is an error, and
;;; whichever is given will be used to calculate the value of the other.

(defclass instrument (linked-named-object)
  ;; the name that should appear on this instrument's staff in the score
  ((staff-name :accessor staff-name :type string :initarg :staff-name 
               :initform nil)
   (staff-short-name :accessor staff-short-name :type string :initarg 
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
   (score-write-bar-line :accessor score-write-bar-line :type integer 
                         :initarg :score-write-bar-line :initform 1)
   (largest-fast-leap :accessor largest-fast-leap :type number 
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
   ;; #'my-fun 
   (chord-function :accessor chord-function :type function 
                   :initarg :chord-function :initform nil)
   ;; we might want to limit our instrument to playing a certain subset of a
   ;; set (for instance to select chords); if so, then set the following slot
   ;; to the id of the subset--obviously this means that this subset would have
   ;; to be present in every set...
   (subset-id :accessor subset-id :initarg :subset-id :initform nil)
   ;; whether the instrument can play microtones or not
   (microtones :accessor microtones :type boolean :initarg :microtones
               :initform nil)
   ;; a list of any notes which the instrument can't play, usually certain
   ;; quarter tones NB These are written notes, not sounding, in the case of
   ;; transposing instruments.
   (missing-notes :accessor missing-notes :type list :initarg :missing-notes
                  :initform nil) 
   ;; 5/3/07: no longer needed (never was used...); chord-function obviates
   ;; this need for this slot.
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
                  Transposition given as ~a which is not ~a ~
                  semitones (the given transposition-semitones)"
                 tr tr-st)))
      (when (and (not tr)
                 (not tr-st))
        (setf (transposition ins) 'c
              (transposition-semitones ins) 0))
      (when (and lw ls)
        (written-sounding-error "lowest-written" "lowest-sounding"))
      (when (and hw hs)
        (written-sounding-error "highest-written" "highest-sounding"))
      (setf (missing-notes ins)
        (loop for note in (missing-notes ins) collect 
              (make-pitch 
               (transpose-note note (transposition-semitones ins)))))
      (if lw
          (setf (lowest-sounding ins) (transpose-note
                                       lw (transposition-semitones ins)))
        (setf (lowest-written ins) (transpose-note
                                    ls (- (transposition-semitones ins)))))
      (if hw
          (setf (highest-sounding ins) (transpose-note
                                        hw (transposition-semitones ins)))
        (setf (highest-written ins) (transpose-note
                                     hs (- (transposition-semitones ins)))))
      ;; 5.8.10 we say in the class def that these will be pitch objects but at
      ;; the mo they're symbols; handle this now and hope it doesn't break
      ;; anything :)
      (setf (lowest-sounding ins) (make-pitch (lowest-sounding ins))
            (lowest-written ins) (make-pitch (lowest-written ins))
            (highest-sounding ins) (make-pitch (highest-sounding ins))
            (highest-written ins) (make-pitch (highest-written ins)))
      (setf (clefs ins) (if (clefs ins)
                            (reverse (clefs ins))
                          (list (starting-clef ins)))
            (clefs-in-c ins) (if (clefs-in-c ins) 
                                 (reverse (clefs-in-c ins))
                               (copy-list (clefs ins))))
      (when pn
        (unless (or (prefers-high ins)
                    (prefers-low ins))
          (error "instrument::initialize-instance: prefers-notes should be ~
                  either nil, 'high or 'low")))
      (when (and (chords ins)
                 (not (chord-function ins)))
        (setf (chord-function ins) 'default-chord-function)))))

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
  (unless (member (starting-clef ins) 
                  '(treble alto tenor bass percussion double-bass 
                    double-treble))
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

(defmethod transposing-instrument-p ((ins instrument) 
                                     &optional (ignore-octaves t))
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

;;; 5.2.11 
(defmethod set-prefers-low ((ins instrument) &optional ignore) 
  (declare (ignore ignore))
  (setf (prefers-notes ins) 'low))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5.2.11 
(defmethod set-prefers-high ((ins instrument) &optional ignore) 
  (declare (ignore ignore))
  (setf (prefers-notes ins) 'high))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod prefers-low ((ins instrument))
  (eq (prefers-notes ins) 'low))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod prefers-high ((ins instrument))
  (eq (prefers-notes ins) 'high))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* instrument/in-range
;;; FUNCTION
;;; in-range: checks whether a pitch is in the range of the instrument or not.
;;; 
;;; ARGUMENTS:
;;; - the instrument instance
;;; - the pitch (object or symbol)
;;; - (optional default nil) whether this is checking the sounding or written
;;; range (default i.e. nil is written)
;;; 
;;; RETURN VALUE: 
;;; t or nil; 0 or 1 as second value depending on whether too high (1) or too
;;; low (0)
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

(defun make-instrument (id &key staff-name
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
;;; default-chord-function:
;;; 
;;; If an instrument is able to play chords, we need to define a function to
;;; select notes from a list that it can play as a chord.  This function (as
;;; a symbol) is passed as a slot to the instrument instance.
;;; 
;;; This is the default function; it returns a 2-note chord with the note at
;;; index plus that below it, or that above it if no notes are below.  Or just
;;; return a single-note chord if neither of those cases are possible.
;;;
;;; ARGUMENTS:
;;; The chord-function defined for an instrument should take six arguments: 
;;; - the current number from the pitch curve
;;; - the index that this was translated into by the offset and
;;;   scaler (based on trying to get a best fit for the instrument and set).
;;;   This can be assumed to be a legal reference into pitch-list as it was
;;;   calculated as fitting in pitch-seq::get-notes.
;;; - the pitch-list that we created from the set, taking the instrument's range
;;;   and other notes already played by other instruments
;;; - the current pitch-seq object                        
;;; - the current instrument object
;;; - the current set object.
;;; 
;;; RETURN VALUE: 
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
    (when (and p2 (> (pitch- p2 p1) 12))  ; assuming pitch-list is sorted
      (setf p2 nil))
    (if p2
        (make-chord (list p1 p2))
      (make-chord (list p1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF instrument.lsp
