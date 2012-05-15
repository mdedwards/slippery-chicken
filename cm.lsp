;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/cm
;;; NAME 
;;; cm
;;;
;;; File:             cm.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          1.0.0-beta1
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of common-music related and other functions
;;;                   like transposition of notes/chords, enharmonic
;;;                   equivalents etc.   
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    1st March 2001
;;;
;;; $$ Last modified: 21:06:48 Mon May 14 2012 BST
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

;;; SAR Thu Mar  1 15:16:40 GMT 2012: Added robodoc entry

;;; MDE Thu Feb  9 14:25:34 2012 
;;; e.g. (in-scale :quarter-tone) (in-scale :chromatic)

;;; ****f* cm/in-scale
;;; FUNCTION
;;; Set the global scale (tuning) for the current slippery-chicken
;;; environment. Current options are :chromatic, :quarter-tone or
;;; :twelfth-tone. See the file cm-load.lsp for specifications and the html
;;; manual page "More about note-names and scales" for more details on use.
;;; 
;;; ARGUMENTS
;;; - A scale (tuning) designation.
;;; 
;;; RETURN VALUE
;;; Lisp REPL feedback on the tuning now set.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)

=> #<tuning "chromatic-scale">

(in-scale :quarter-tone)

=> #<tuning "quarter-tone">

(in-scale :twelfth-tone)

=> #<tuning "twelfth-tone">

|#
;;; SYNOPSIS
(defun in-scale (scale)
;;; ****
  (let ((sc (if (eq scale :chromatic)
                cm::*chromatic-scale*
                (cm::find-object scale))))
    (unless sc 
      (error "cm.lsp::in-scale: Can't find scale ~a." scale))
    (setf cm::*scale* sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 18:44:41 BST 2012: Added robodoc entry

;;; ****f* cm/degrees-per-octave
;;; FUNCTION
;;; Return the number of scale degrees in the span of one octave within the
;;; current tuning system.
;;; 
;;; ARGUMENTS
;;; - No arguments.
;;; 
;;; RETURN VALUE
;;; - An integer that is the number of scale degrees in each octave.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(degrees-per-octave)

=> 12

(in-scale :quarter-tone)
(degrees-per-octave)

=> 24

|#
;;; SYNOPSIS
(defun degrees-per-octave ()
;;; ****
  (- (cm::keynum 'cm::c5) (cm::keynum 'cm::c4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 18:49:36 BST 2012: Added robodoc entry

;;; ****f* cm/degree-to-note
;;; FUNCTION

;;; Get the specified scale degree number as a note-name pitch symbol within
;;; the current scale. An optional argument allows the user to specify that the
;;; scale degree number should be used to get the note-name pitch from a
;;; different scale. 
;;; 
;;; ARGUMENTS
;;; An integer that is a scale degree number.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The scale from which the note-name pitch symbol associated with the
;;;   specified scale degree is to be drawn. As this is a cm function, the cm
;;;   tuning names must be used; i.e., cm::*chromatic-scale*. 
;;; 
;;; RETURN VALUE
;;; A note-name pitch symbol.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(degree-to-note 127)

=> G9

(in-scale :twelfth-tone)
(degree-to-note 127)

=> ATS0

(in-scale :quarter-tone)
(degree-to-note 127)

=> EQF4

|#
;;; SYNOPSIS
(defun degree-to-note (degree &optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (cm::find-object scale)))
  (rm-package (cm::note (round degree) :in scale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:07:36 BST 2012: Added robodoc entry

;;; ****f* cm/midi-to-degree
;;; FUNCTION
;;; Convert the specified MIDI note number to the degree number of the current
;;; scale. 
;;; 
;;; ARGUMENTS
;;; - A MIDI note number.
;;; 
;;; RETURN VALUE
;;; - An integer that is the scale-degree equivalent of the specified MIDI note
;;;   number in the current scale.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(midi-to-degree 64)

=> 64

(in-scale :twelfth-tone)
(midi-to-degree 64)

=> 384

(in-scale :quarter-tone)
(midi-to-degree 64)

=> 128

|#
;;; SYNOPSIS
(defun midi-to-degree (midi-note)
;;; ****
  (* midi-note (/ (degrees-per-octave) 12)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:12:48 BST 2012: Added robodoc entry

;;; ****f* cm/midi-to-note
;;; FUNCTION
;;; Get the note-name pitch symbol equivalent of the specified MIDI note
;;; number.
;;; 
;;; ARGUMENTS
;;; - An integer that is a MIDI note number.
;;; 
;;; RETURN VALUE
;;; A note-name pitch symbol.
;;; 
;;; EXAMPLE
#|
(midi-to-note 67)

=> G4

|#
;;; SYNOPSIS
(defun midi-to-note (midi-note)
;;; ****
  (degree-to-note midi-note cm::*chromatic-scale*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:17:30 BST 2012: Added robodoc entry

;;; ****f* cm/midi-to-freq
;;; FUNCTION
;;; Get the frequency equivalent in Hertz to the specified MIDI note number. 
;;; 
;;; ARGUMENTS
;;; - A number (can be a decimal) that is a MIDI note number.
;;; 
;;; RETURN VALUE
;;; A decimal number that is a frequency in Hertz.
;;; 
;;; EXAMPLE
#|
(midi-to-freq 67)

=> 391.99542

(midi-to-freq 67.9)

=> 412.91272

|#
;;; SYNOPSIS
(defun midi-to-freq (midi-note)
;;; ****
  (cm::hertz midi-note :in cm::*chromatic-scale*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:22:20 BST 2012: Added robodoc entry

;;; ****f* cm/note-to-midi
;;; FUNCTION
;;; Get the MIDI note number equivalent for a chromatic note-name pitch
;;; symbol. 
;;; 
;;; ARGUMENTS
;;; - A chromatic note-name pitch symbol.
;;; 
;;; RETURN VALUE
;;; An integer.
;;; 
;;; EXAMPLE
#|
(note-to-midi 'g4)

=> 67

|#
;;; SYNOPSIS
(defun note-to-midi (midi-note)
;;; ****
  (note-to-degree midi-note cm::*chromatic-scale*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:25:41 BST 2012: Added robodoc entry

;;; ****f* cm/degrees-to-notes
;;; FUNCTION
;;; 
;;;
;;; NB: If the specified scale-degree number within the current scale would
;;;     result in pitch outside of the maximum MIDI pitch range for that tuning
;;;     (chromatic: C-1 to B10; quarter-tone: C-1 to BQS10; twelfth-tone: C-1
;;;     to CTF11), the function will return an error.
;;; 
;;; ARGUMENTS
;;; An integer that is a scale degree number in the current tuning.
;;; 
;;; RETURN VALUE
;;; A list of note-name pitch symbols.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(degrees-to-notes '(0 143 116 127 38))

=> (C-1 B10 AF8 G9 D2)

(in-scale :twelfth-tone)
(degrees-to-notes '(0 144 116 127 38 287 863))

=> (C-1 C1 GSS0 ATS0 FSSS-1 CTF3 CTF11)

(in-scale :quarter-tone)
(degrees-to-notes '(0 144 116 127 38 287))

=> (C-1 C5 BF3 EQF4 G0 BQS10)

|#
;;; SYNOPSIS
(defun degrees-to-notes (degrees)
;;; ****
  (loop for d in degrees collect (degree-to-note d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:41:37 BST 2012: Added robodoc entry

;;; ****f* cm/freq-to-note
;;; FUNCTION
;;; Get the note-name pitch equivalent of the specified frequency, rounded to
;;; the nearest scale degree of the current scale.
;;; 
;;; ARGUMENTS
;;; A number that is a frequency in Hertz.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The scale in which the note-name pitch equivalent is to be sought.
;;; 
;;; RETURN VALUE
;;; A note-name pitch symbol.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(freq-to-note 423)

=> AF4

(in-scale :twelfth-tone)
(freq-to-note 423)

=> GSSS4

(in-scale :quarter-tone)
(freq-to-note 423)

=> AQF4

|#
;;; SYNOPSIS
(defun freq-to-note (freq &optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (cm::find-object scale)))
  (rm-package (cm::note freq :hz t :in scale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:49:28 BST 2012: Added robodoc entry

;;; ****f* cm/note-to-freq
;;; FUNCTION
;;; Get the frequency in Hertz of the specified note-name pitch symbol.
;;; 
;;; ARGUMENTS
;;; - A note-name pitch symbol.
;;; 
;;; RETURN VALUE
;;; A frequency in Hertz.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(note-to-freq 'AF4)

=> 415.3047

(in-scale :twelfth-tone)
(note-to-freq 'GSSS4)

=> 423.37845

(in-scale :quarter-tone)
(note-to-freq 'AQF4)

=> 427.47403

|#
;;; SYNOPSIS
(defun note-to-freq (note)
;;; ****
  (cm::hertz (rm-package note :cm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:55:06 BST 2012: Added robodoc entry

;;; ****f* cm/note-to-degree
;;; FUNCTION

;;; Get the scale degree number of the specified note-name pitch symbol within
;;; the current scale. 
;;; 
;;; ARGUMENTS
;;; - A note-name pitch symbol.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The scale in which to find the scale-degree of the specified pitch.
;;; 
;;; RETURN VALUE
;;; An integer that is a scale degree in the current scale.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(note-to-degree 'AF4)

=> 68

(in-scale :twelfth-tone)
(note-to-degree 'AF4)

=> 408

(in-scale :quarter-tone)
(note-to-degree 'AF4)

=> 136

|#
;;; SYNOPSIS
(defun note-to-degree (note &optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (cm::find-object scale)))
  (cm::keynum (rm-package note :cm) :in scale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 20:01:30 BST 2012: Added robodoc entry

;;; Defaults to *scale*

;;; ****f* cm/freq-to-degree
;;; FUNCTION
;;; Get the scale degree of the specified frequency in Hertz within the current
;;; scale. 
;;; 
;;; NB: This method will return fractional scale degrees.
;;; 
;;; ARGUMENTS
;;; A frequncy in Hertz.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The scale in which to find the corresponding scale degree.
;;; 
;;; RETURN VALUE
;;; A scale degree number. This may be a decimal number.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(freq-to-degree 423)

=> 68.317856

(in-scale :twelfth-tone)
(freq-to-degree 423)

=> 409.9071

(in-scale :quarter-tone)
(freq-to-degree 423)

=> 136.63571

|#
;;; SYNOPSIS
(defun freq-to-degree (degree &optional (chromatic-scale nil))
;;; ****
  (declare (special cm::*chromatic-scale*))
  (cm::keynum degree :hz t :in (if chromatic-scale 
                                 cm::*chromatic-scale*
                               cm::*scale*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 20:08:11 BST 2012: Added robodoc entry

;;; MDE Sat Jan  7 18:06:01 2012 -- this will always return >= 0 <= 1.0 so a
;;; bend upwards from the nearest chromatic note below our freq 

;;; ****f* cm/get-pitch-bend
;;; FUNCTION
;;; Get the MIDI pitch-bend value necessary for application to a MIDI pitch in
;;; order to achieve the specified frequency.
;;;
;;; NB: This will always return a positive value between 0.0 and 1.0, as
;;;     slippery-chicken always applies pitch-bends upwards from the nearest
;;;     chromatic note below the specified frequency. 
;;;
;;; NB: This value will be the same in all tuning scales.
;;; 
;;; ARGUMENTS
;;; A frequency in Hertz.
;;; 
;;; RETURN VALUE
;;; A two-digit decimal number that is the pitch-bend value required to achieve
;;; the specified frequency in MIDI.
;;; 
;;; EXAMPLE
#|
(get-pitch-bend 423)

=> 0.32

|#
;;; SYNOPSIS
(defun get-pitch-bend (freq)
;;; ****
  (let* ((chromatic-degree (freq-to-degree freq t))
         (rem (rem chromatic-degree 1)))
    ;; float discrepancies result in e.g. c#4 being 60.999996....
    (if (or (equal-within-tolerance 1 rem .005) ; used to be .0001
            (equal-within-tolerance 0 rem .005))
        0.0
        ;; rem)))
        ;; MDE Sat Jan  7 00:39:10 2012 -- instead of the above try
        ;; rounding to 0.01 (1 cent) 
        (/ (round rem 0.01) 100.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;;; obsolete
(defun list-midi-events (file)
  (cm::list-objects (cm::import-events file)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-qtr-flat (note)
 (search "QF" (string (cm::note (rm-package note :cm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-qtr-sharp (note)
  (search "QS" (string (cm::note (rm-package note :cm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-sharp (note)
  (let ((str (string (cm::note (rm-package note :cm)))))
    (when (and (equal #\S (elt str 1))
               (digit-char-p (elt str 2)))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-flat (note)
  (let ((str (string (cm::note (rm-package note :cm)))))
    (when (and (equal #\F (elt str 1))
               (digit-char-p (elt str 2)))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B. won't work with bracketed accidentals of the form cbn3!

(defun is-natural (note)
  (let ((2nd-char (elt (string note) 1)))
    (or (numberp (digit-char-p 2nd-char))
        (equal 2nd-char #\N))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 11:31:58 BST 2012: Added robodoc entry

;;; MDE Sat Feb 11 10:41:11 2012 

;;; ****f* cm/degrees-per-semitone
;;; FUNCTION
;;; Get the number of scale degrees per equal-tempered semitone in the current
;;; tuning scale. 
;;; 
;;; ARGUMENTS
;;; - No arguments
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The scale for which the number of degrees per semitone is to be
;;;   retrieved. 
;;; 
;;; RETURN VALUE
;;; An integer.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)
(degrees-per-semitone)

=> 1

(in-scale :twelfth-tone)
(degrees-per-semitone)

=> 6

(in-scale :quarter-tone)
(degrees-per-semitone)

=> 2

|#
;;; SYNOPSIS
(defun degrees-per-semitone (&optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (cm::find-object scale)))
  (- (note-to-degree 'cs4 scale)
     (note-to-degree 'c4 scale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-note (note semitones &optional (package nil))
  (when (and note semitones)
    (let ((n note))
      (when (listp n) ;; could be (c1 natural)
        (setf n (first n)))
      (setf n (rm-package n :sc))
      (let* ((degrees-per-semitone (degrees-per-semitone))
             (degrees (floor (* semitones degrees-per-semitone)))
             (degree (note-to-degree n)))
        (unless degree
          (error "scale::transpose-note: ~a is not a valid note!" note))
        (setf n (rm-package (cm::note (+ degree degrees))
                            (if package package :sc)))
        (if (listp note)
            (cons n (rest note))
          n)))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-chord (note-list semitones &optional (package nil))
  (if (zerop semitones) 
      note-list
    (loop for n in note-list collect (transpose-note n semitones package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tranpose-chords (chords semitones)
  (loop for chord in chords collect (transpose-chord chord semitones)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Assumes downward transposition.

(defun transposition-to-semitones (transp &optional (warn t))
  (declare (special cm::*chromatic-scale*))
  (let* ((c4 60)
         (note (read-from-string (format nil "cm::~a3" transp))))
    (when warn 
      (warn "cm::transposition-to-semitones: ~
             Assuming downward transposition: ~a"
            transp))
    (- (note-to-degree note cm::*chromatic-scale*) c4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun semitones-to-transposition (st)
  (declare (special cm::*chromatic-scale*))
  (let ((result (cm::note (+ 60 st) :in cm::*chromatic-scale*)))
    ;; CM always gives sharps in preference to flats when you call note,
    ;; whereas tranpositions are usually flats (eg horn in e flat not d sharp)
    (if (search "S" (string result))
        (values (get-note-octave (enharmonic-equivalent result)))
      (values (get-note-octave result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this function returns the enharmonic of a note.  it doesn't handle notes
;;; explicitly designated natural (cn gn etc.) or double sharps or flats.
;;; (enharmonic 'gs5) => af5

(let ((enharmonics '((c bs) (cs df) (ds ef) (e ff) (f es) (fs gf) (gs af)
                     (as bf) (b cf) 
                     ;; quarter-tones
                     (cqf bqs) (eqs fqf)
                     ;; just return the same note if there is no enharmonic
                     (d d) (g g) (a a))))
  (defun enharmonic-equivalent (note &optional (warn t))
    (multiple-value-bind (symbol octave)
        (get-note-octave (rm-package note :cm))
      (let ((enh (loop 
                     for pair in enharmonics ; better as position with :test?
                     for pos = (position (rm-package symbol) pair)
                     when pos return
                       (if (zerop pos)
                           (second pair)
                         (first pair)))))
        (when (and (not enh)
                   warn)
          (warn "scale::enharmonic-equivalent: ~
                 Couldn't find enharmonic to ~a"
                note))
        (when enh
          (when (or (eq symbol 'cf) (eq symbol 'cqf) (eq symbol 'c))
            (decf octave))
          (when (or (eq symbol 'bs) (eq symbol 'bqs) (eq symbol 'b))
            (incf octave))
          (read-from-string (format nil "~a~a" enh octave)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun join-note-octave (note octave)
  (read-from-string (format nil "~a~a" note octave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((last-octave nil))
  (defun get-note-octave (note &optional use-last-octave)
    (let* ((string (string note))
           (first-digit-pos (loop for i below (length string)
                               for current = (elt string i)
                               ;; 22.10.11 could have negative octaves!
                               when (or (char-equal current #\-)
                                        (digit-char-p current))
                               return i))
           (just-note (when first-digit-pos
                        (read-from-string string :start 0 
                                          :end first-digit-pos)))
           (octave (when first-digit-pos
                     (parse-integer string :start first-digit-pos))))
      (if just-note
          (progn
            (setf last-octave octave)
            (values just-note octave))
          ;; there was no octave given!
          (if use-last-octave
              (progn
                (unless last-octave
                  (error "cm::get-note-octave: no last octave!"))
                (values note last-octave))
              (values note nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun midi-time-sigs (file &optional (end 99999999) (track 0))
  (let ((events (cm::import-events file :tracks track)))
    (loop for e in (cm::subobjects events) 
       with sum = 0 
       while (<= (cm::object-time e) end)
       do
       (when (typep e 'cm::midi-time-signature)
         (let* ((num (cm::midi-event-data1 e))
                (den (cm::midi-event-data2 e)))
           ;; count 8ths...nah only works if there's a time sig for every bar
           (incf sum (* num (/ 8 den)))
           (print e)))
       finally (format t "~%~a 8ths total" sum))))

;;; get all the time signatures from a midi file--all bars, not just changes.
;;; NB This only works if tempi are at the beginnings of bars so if you see
;;; errors and your MIDI file was exported from e.g. sibelius, it could be that
;;; your tempo change was attached mid-bar.  It also misses the last bars from
;;; the last time-sig or tempo change.
(defun midi-time-sigs-all-bars (file &optional (end 99999999) (track 0))
  (let ((events (cm::import-events file :tracks track)))
    (loop for e in (cm::subobjects events) 
       for time = (cm::object-time e)
       ;; with sum = 0 
       with last-ts
       with new-ts
       with last-time = 0
       with last-tempo = -1
       with new-tempo = -1
       ;; with num-bars = 0
       with result = '()
       with calc = nil
       with total-bars = 0
       while (<= time end)
       do
       ;; Ignore tempi if they're the same as the current.  First time we see a
       ;; time-sig, last-ts is nil so just store in new-ts and last-ts),
       ;; don't count bars; sim for tempo: last-tempo will be -1 so just
       ;; store in last-tempo and new-tempo.  From then on, if we see a new
       ;; time-sig or tempo, store in new-ts or new-tempo, and set calc to t
       ;; so that we calculate how many bars have passed.  The calcuation
       ;; processes uses last-ts and last-tempo, then sets calc to nil and
       ;; last-ts to new-ts, last-tempo to new-tempo--always both are set.
       ;; This means that unless we get e.g. a new tempo, then last-tempo
       ;; and new-tempo remain the same each time we get a new
       ;; time-sig--this is exactly what we want.  Same for a true new
       ;; tempo: new-ts and last-ts will become the same, which is quite
       ;; right.
       (when (typep e 'cm::midi-time-signature)
         (let* ((num (cm::midi-event-data1 e))
                (den (cm::midi-event-data2 e))
                (ts (list num den)))
           (if last-ts
               ;; got a new one so calculate how many we've had of the old
               (setf calc t
                     new-ts ts)
               ;; this is the first ts we've seen
               (setf last-ts ts
                     new-ts ts))))
       (when (typep e 'cm::midi-tempo-change)
         (let ((tempo (/ 60000000.0 (cm::midi-event-data1 e))))
           (unless (= new-tempo tempo)
             (if (> last-tempo 0)
                 ;; got new tempo so work out how many bars we've had up to now
                 (setf calc t
                       new-tempo tempo)
                 ;; this is the first tempo we've seen
                 (setf last-tempo tempo
                       new-tempo tempo)))))
       ;; which order do we see things in? don't add bars twice, once for ts
       ;; then for tempo when they're both at same time...
       (when calc 
         (let ((nb (mtsab-aux last-ts last-time time last-tempo)))
           (incf total-bars nb)
           ;; (format t "~&~a bars of ~a @ ~a" nb last-ts time)
           (loop repeat nb do (push last-ts result)))
         (setf calc nil
               last-time time
               last-tempo new-tempo
               last-ts new-ts))
       finally 
       (format t "~&~a bars total" total-bars)
       (return (nreverse result)))))

;; how many bars of time-sig
(defun mtsab-aux (time-sig last-time this-time tempo)
  (let* ((ts (make-time-sig time-sig))
         (time (- this-time last-time))
         (bar-dur (* (/ 60 tempo) (duration ts)))
         (num-bars (/ time bar-dur)))
    ;; can't have fractional bars...
    (multiple-value-bind
          (nb rem)
        (round num-bars)
      (when (> rem 0.001)
        (error "mtsab-aux: somehow got fractional bars: ~a ~
               (rem ~a, this-time ~a last-time ~a tempo ~a time-sig ~a)" 
               num-bars rem this-time last-time tempo time-sig))
      nb)))
         

;;; Sadly the last event in the file doesn't give us the full duration... :/
(defun midi-tempo-curve (file duration &optional (track 0))
  (let* ((events (cm::import-events file :tracks track))
         tempo
         (result 
          (progn 
            (unless events
              (error "no events in ~a" file))
            (loop for e in (cm::subobjects events)
               when (typep e 'cm::midi-tempo-change)
               do (setf tempo (round (/ 60000000.0 (cm::midi-event-data1 e))))
               and collect (cm::object-time e)
               ;; see midi3.lisp line 85
               and collect tempo
               ;; do
                 ;; (setf last-time (cm::object-time e))
                 ))))
    (midi-tempo-curve-aux (append result (list duration tempo)))))

;;; strangely we get simultaneities in the midi tempo curve e.g. '(0 60 0 60 0
;;; 60 0 52) so get rid of all up until the last one for a given point NB
;;; repeated ys are allowed.
;;; e.g.(midi-tempo-curve-aux
;;;    '(0.0 494 0.00 494 0.00 444 0.00 426 0.00 426 0.9 426 1.8 426 2.1 430 3
;;;      450 3 460 5 460))  
;;; --> (0.0 426 0.9 426 1.8 426 2.1 430 3 460 5 460)

(defun midi-tempo-curve-aux (env)
  (loop for x in env by #'cddr and y in (cdr env) by #'cddr 
     ;; with last-x = most-negative-double-float with last-y =
     ;; most-negative-double-float
     with last-x = (first env) with last-y = (second env)
     ;; do (print x)
     when (/= last-x x)
     collect last-x into result and collect last-y into result
     do (setf last-x x last-y y)
     finally (return (append result (list x y)))))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5/5/06: updated for cm 2.8.0
;;; bend is in semitones

#+cm-2
(defun set-pitch-bend (time channel bend)
  ;; (declare (special midimsg msg))
  ;; (output (new midimsg time time msg 
  ;; (format t "~&set-pitch-bend: time ~a channel ~a bend ~a" time channel bend)
  (new midi-pitch-bend :time time :channel channel
       :bend (rescale bend -2 2 -8192 8191)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
(defun reset-pitch-bend (time channel)
  (set-pitch-bend time channel 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
(defun output-midi-note (midi-note pitch-bend time amplitude duration
                                   channel)  
  (declare (special midi keynum))
  (set-pitch-bend time channel pitch-bend)
  (new midi 
       :time time 
       :keynum midi-note
       :amplitude amplitude
       :duration duration  
       :channel channel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tempo is a tempo instance

(defun output-midi-tempo-change (time tempo)
  ;; (print tempo)
  (new midi-tempo-change :time time :usecs (sc::usecs tempo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-midi-time-sig (time num denom &optional (clocks 24))
  ;; (format t "~&output-midi-time-sig: time: ~a, num: ~a, denom: ~a, ~
  ;;         clocks: ~a"
  ;;      time num denom clocks)
  (new midi-time-signature :time time :numerator num
       :denominator denom :clocks clocks))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
(defun midi-program-change (time channel program)
  ;; (format t "~&midi-program-change: time: ~a, channel: ~a, program: ~a"
     ;;     time channel program)
  (new midi-program-change :time time :channel (1- channel)
       :program (1- program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5/5/06: updated for cm 2.8.0

#+cm-2
(defun midi-program-changes (voices &optional (time 0.0))
  (new seq :name 'program-changes :time time
       :subobjects
       (loop for v in voices collect
             (midi-program-change time (first v) (second v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; voices is the type of list structure returned by
;;; slippery-chicken::get-events-start-time-duration  
;;; midi-setup are the channels and program numbers for the different
;;; instruments.  
;;; start-tempo is an sc tempo object

(defun process-voices (voices midi-file start-tempo midi-setup time-offset
                       &optional force-velocity)
  (events
   (cons (midi-program-changes midi-setup)
         (loop for voice in voices collect
              (new seq :name (gensym) :time 0.0 :subobjects
                   (loop for rs in voice appending
                        (loop for event in rs 
                           appending
                           (sc::output-midi event time-offset 
                                            force-velocity))))))
   midi-file :tempo (sc::qtr-bpm start-tempo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; start-tempo is a bpm
(defun event-list-to-midi-file (event-list midi-file start-tempo time-offset
                                &optional force-velocity)
  (events
   (new seq :name (gensym) :time 0.0 :subobjects
        (loop for event in (sc::sort-event-list event-list)
           appending
           (sc::output-midi event time-offset 
                            force-velocity)))
   midi-file :tempo start-tempo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (cm::parse-midi-file "/Users/medward2/mus/altogether/score/altogether.mid")

;;; ****f* cm/parse-midi-file
;;; FUNCTION
;;; Print the MIDI event slots in the specified file to the Lisp listener.
;;;
;;; NB: This is a Common Music function and as such must be called with the
;;;     package qualifier cm:: if used within slippery chicken.
;;; 
;;; ARGUMENTS 
;;; - The path (including the file name) to the MIDI file.
;;;
;;; OPTIONAL ARGUMENTS
;;; - An integer or NIL to indicate which track in the specified MIDI file is
;;;   to be accessed. If NIL, all tracks will be accessed. NB: CM (and
;;;   therefore slippery-chicken too) generates some MIDI files by writing each
;;;   channel to a different track, so the "track" would seem synonymous with
;;;   "channel" here. 
;;;
;;; RETURN VALUE  
;;; The CM data for the MIDI events in the specified file.
;;;
;;; EXAMPLE
#|
(cm::parse-midi-file "/tmp/multi-ps.mid")

=>
Event #i(midi-tempo-change time 0.0 usecs 1000000)
Event #i(midi time 0.0 keynum 72 duration 0.5 amplitude 0.6929134 channel 0)
Event #i(midi time 0.0 keynum 65 duration 0.5 amplitude 0.6929134 channel 1)
Event #i(midi time 0.0 keynum 60 duration 0.5 amplitude 0.6929134 channel 2)
Event #i(midi-time-signature time 0.0 numerator 4 denominator 4 clocks 24 32nds 8)
Event #i(midi-time-signature time 0.0 numerator 4 denominator 4 clocks 24 32nds 8)
Event #i(midi-time-signature time 0.0 numerator 4 denominator 4 clocks 24 32nds 8)
Event #i(midi-tempo-change time 0.0 usecs 1000000)
Event #i(midi-tempo-change time 0.0 usecs 1000000)
Event #i(midi-tempo-change time 0.0 usecs 1000000)
Event #i(midi time 0.5 keynum 67 duration 0.5 amplitude 0.6929134 channel 2)
Event #i(midi time 0.5 keynum 71 duration 0.5 amplitude 0.6929134 channel 0)
Event #i(midi time 0.5 keynum 64 duration 0.5 amplitude 0.6929134 channel 1)
Event #i(midi time 1.0 keynum 60 duration 0.5 amplitude 0.6929134 channel 2)
Event #i(midi time 1.0 keynum 72 duration 0.5 amplitude 0.6929134 channel 0)
Event #i(midi time 1.0 keynum 62 duration 0.5 amplitude 0.6929134 channel 1)
Event #i(midi time 1.5 keynum 67 duration 0.5 amplitude 0.6929134 channel 2)
Event #i(midi time 1.5 keynum 71 duration 0.5 amplitude 0.6929134 channel 0)
Event #i(midi time 1.5 keynum 64 duration 0.5 amplitude 0.6929134 channel 1)
Event #i(midi time 2.0 keynum 60 duration 0.5 amplitude 0.6929134 channel 2)
Event #i(midi time 2.0 keynum 72 duration 0.5 amplitude 0.6929134 channel 0)
Event #i(midi time 2.0 keynum 65 duration 0.5 amplitude 0.6929134 channel 1)
Event #i(midi time 2.5 keynum 67 duration 0.5 amplitude 0.6929134 channel 2)
Event #i(midi time 2.5 keynum 71 duration 0.5 amplitude 0.6929134 channel 0)
Event #i(midi time 2.5 keynum 64 duration 0.5 amplitude 0.6929134 channel 1)
Event #i(midi time 3.0 keynum 60 duration 0.5 amplitude 0.6929134 channel 2)
Event #i(midi time 3.0 keynum 72 duration 0.5 amplitude 0.6929134 channel 0)
Event #i(midi time 3.0 keynum 62 duration 0.5 amplitude 0.6929134 channel 1)
Event #i(midi time 3.5 keynum 67 duration 0.5 amplitude 0.6929134 channel 2)
Event #i(midi time 3.5 keynum 71 duration 0.5 amplitude 0.6929134 channel 0)
Event #i(midi time 3.5 keynum 64 duration 0.5 amplitude 0.6929134 channel 1)
31 events total

|#
;;; 
;;; SYNOPSIS
(defun parse-midi-file (file &optional track)
;;; ****
  (let ((midi-stream (parse-midi-file-aux file track))
        (num-events 0))
    (setf num-events (length (subobjects midi-stream)))
    (map-subobjects (lambda (n) (format t "~&Event ~A" n)) 
                    midi-stream)
    (format t "~&~a events total" num-events)
    num-events))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 11:48:51 BST 2012: Added robodoc entry

;;; ****f* cm/midi-file-high-low
;;; DATE
;;; 30-Dec-2010
;;;
;;; FUNCTION
;;; Print the highest and lowest pitch in a specified MIDI file as a MIDI note
;;; number. 
;;;
;;; NB: This is a Common Music function and as such must be called with the
;;;     package qualifier cm:: if used within slippery chicken.
;;; 
;;; ARGUMENTS 
;;; - The path (including the name) to the MIDI file.
;;;
;;; OPTIONAL ARGUMENTS
;;; - An integer or NIL to indicate which track in the specified MIDI file is
;;;   to be accessed. If NIL, all tracks will be accessed. NB: CM (and
;;;   therefore slippery-chicken too) generates some MIDI files by writing each
;;;   channel to a different track, so the "track" would seem synonymous with
;;;   "channel" here. 
;;; 
;;; RETURN VALUE  
;;; Two integer values (using the values function) that are the highest and
;;; lowest pitches in the specified MIDI file.
;;;
;;; EXAMPLE
#|
(cm::midi-file-high-low "/tmp/multi-ps.mid")

=> 72, 60

|#
;;; 
;;; SYNOPSIS
(defun midi-file-high-low (file &optional track)
;;; ****
  (let ((midi-stream (parse-midi-file-aux file track))
        (low 128)
        (high 0))
    (map-subobjects (lambda (n) 
                      (let ((note (midi-keynum n)))
                        (when (< note low)
                          (setf low note))
                        (when (> note high)
                          (setf high note))))
                    midi-stream :type 'midi)
    (format t "~&high: ~a low: ~a" 
            (sc::midi-to-note high) (sc::midi-to-note low))
    (values high low)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun midi-file-to-events-list (file &optional track)
  (let ((midi-stream (parse-midi-file-aux file track)))
    (subobjects midi-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; btw the time slot is cm::object-time, midi note number cm::midi-keynum
(defun parse-midi-file-aux (file &optional track)
  (let ((midi-stream (import-events file)))
    (when track
      (setf midi-stream (nth track midi-stream)))
    midi-stream))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 11:59:43 BST 2012: Conformed robodoc entry

;;; ****f* cm/midi-file-one-note
;;; FUNCTION
;; write all midi notes in the file out to a new file using the single note
;; <note> and <channel>.
;;; 
;;; ARGUMENTS 
;;; - the file path as a string
;;; - the note to write (symbol or midi note number)
;;; - the new channel to write note to (counting from 1)
;;;
;;; OPTIONAL ARGUMENTS 

;;; - the old channel: if given, only notes on this channel will be written
;;;   (counting from 1).
;;; 
;;; RETURN VALUE  
;;; the path to the new file
;;; 
;;; EXAMPLE
;;; (cm::midi-file-one-note 
;;;         "/Users/medward2/mus/altogether/altogether.mid" 'c4 9)
;;; 
;;; SYNOPSIS
(defun midi-file-one-note (file note channel &optional old-channel)
;;; ****
  ;; MDE Sun May  6 16:52:09 2012 -- check
  (when (integerp channel)
    (decf channel))
  ;; MDE Sun May  6 16:50:33 2012 -- test for value first
  (when (integerp old-channel)
    (decf old-channel))
  (let ((midi-stream (import-events file))
        (degree (if (numberp note) note (keynum note)))
        (new-file (format nil "~a-one-note.mid"
                          (sc::path-minus-extension file)))
        (new '()))
    (map-subobjects (lambda (n) 
                      (when (or (not old-channel)
                                (= old-channel (midi-channel n)))
                        (setf (midi-channel n) channel)
                        (setf (midi-keynum n) degree)
                        (push n new)))
                    midi-stream :type 'midi)
    (if new
        (events (nreverse new) new-file)
        (warn "cm::midi-file-one-note::No events matched/written."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cm.lsp

