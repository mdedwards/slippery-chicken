;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/cm
;;; NAME 
;;; cm
;;;
;;; File:             cm.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          1.1.0
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
;;; $$ Last modified:  14:28:49 Tue May 20 2025 CEST
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Thu Mar  1 15:16:40 GMT 2012: Added robodoc entry

;;; MDE Thu Feb  9 14:25:34 2012 
;;; e.g. (in-scale :quarter-tone) (in-scale :chromatic)

;;; ****f* cm/in-scale
;;; DESCRIPTION
;;; Set the global scale (tuning) for the current slippery-chicken
;;; environment. Current options are :chromatic, :quarter-tone or
;;; :twelfth-tone. See the file cm-load.lsp for specifications and the html
;;; manual page "More about note-names and scales" for more details on use.
;;; 
;;; ARGUMENTS
;;; - A scale (tuning) designation.
;;; 
;;; RETURN VALUE
;;; the (new) Common Music scale object
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
                (find-scale scale))))
    (when sc
      (setf cm::*scale* sc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Jun 15 14:55:25 2012 
(defun find-scale (scale)
  (if (typep scale 'cm::tuning)
      scale
      (let ((sc (cm::find-object scale)))
        (unless sc 
          (warn "cm.lsp::find-scale: Can't find scale ~a." scale))
        sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We use low-a-freq rather than a low C freq because it's (usually) easily
;;; divisible and thus causes no visible rounding errors.

;;; ****f* cm/set-diapason
;;; DATE
;;; March 11th 2021
;;; 
;;; DESCRIPTION
;;; Set the frequency of the note A4 (the diapason). By default, in slippery
;;; chicken (and common music) this is 440 Hertz. If this function is called, it
;;; will change the diapason for all currently defined scales, and thus will
;;; affect all calculations in any scale that converts to or from frequency.
;;;
;;; NB Explicit calling of this function is discouraged. Use instead something
;;; like this: (set-sc-config 'diapason 442)
;;; 
;;; ARGUMENTS
;;; - the new frequency of the note A4 (middle A: i.e. the note played by the
;;;   oboe when a western orchestra tunes). (For internal use, if this is NIL,
;;;   the frequency of the lowest A will be returned.)
;;; 
;;; RETURN VALUE
;;; The frequency in Hertz of the lowest A in the (new) tuning.
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(let ((low-a 440/64))                   ; 6.875Hz (lowest A)
  (defun set-diapason (hertz)
;;; ****
    (declare (special +slippery-chicken-config-data+))
    (if (not hertz)
        low-a
        (let ((new-low-a (/ hertz 64)))
          (setq low-a new-low-a)
          ;; just in case the user does explicitly call this function. NB don't
          ;; call set-sc-config as that will call this function again, never
          ;; ending.
          (replace-data 'diapason hertz +slippery-chicken-config-data+)
          ;; it would be nice to do this:
          ;;
          ;; (loop for scale in (cm::list-named-objects 'cm::tuning) do
          ;;    (setf (cm::scale-lowest scale) new-low-a))
          ;;
          ;; but I can't see a mechanism to have all of CM's scale data
          ;; affected/recalculated by setting just one slot (almost certainly
          ;; for efficiency's sake). Even the :cents slots we provide for
          ;; tunings get deleted at init and replaced by the freq scaler for a
          ;; degree shift, I believe. So the easiest thing to do is change the
          ;; low A freq and reload the tuning file.
          ;; (cl-user::sc-compile-and-load "cm-load.lsp" t)
          (load (file-from-sc-dir "src/cm-load.lsp"))
          new-low-a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* cm/degrees-per-octave
;;; DESCRIPTION
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
(defun degrees-per-octave (&optional (scale cm::*scale*))
;;; ****
  (- (cm::keynum 'cm::c5 :in scale) (cm::keynum 'cm::c4 :in scale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 18:49:36 BST 2012: Added robodoc entry

;;; ****f* cm/degree-to-note
;;; DESCRIPTION
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
;;;   specified scale degree is to be drawn.
;;; 
;;; RETURN VALUE
;;; A note-name pitch symbol.
;;; 
;;; EXAMPLE
#|
(degree-to-note 127 'chromatic-scale)

=> G9

(degree-to-note 127 'twelfth-tone)

=> ATS0

(degree-to-note 127 'quarter-tone)

=> EQF4

|#
;;; SYNOPSIS
(defun degree-to-note (degree &optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (find-scale scale)))
  (rm-package (cm::note (round degree) :in scale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:07:36 BST 2012: Added robodoc entry

;;; ****f* cm/midi-to-degree
;;; DESCRIPTION
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
(defun midi-to-degree (midi-note &optional (scale cm::*scale*))
;;; ****
  (* midi-note (/ (degrees-per-octave scale) 12)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 19:12:48 BST 2012: Added robodoc entry

;;; ****f* cm/midi-to-note
;;; DESCRIPTION
;;; Get the note-name pitch symbol equivalent of the specified MIDI note
;;; number. 
;;; 
;;; ARGUMENTS
;;; - a MIDI note number: integer or float. If float, appropriate microtones
;;; will be returned if the current scale is microtonal (e.g. (in-scale
;;; :quarter-tone)) 
;;; 
;;; RETURN VALUE
;;; A note-name pitch symbol.
;;; 
;;; EXAMPLE
#|
(midi-to-note 67)
=> G4
(midi-to-note 60.5)
=>CQS4
|#
;;; SYNOPSIS
(defun midi-to-note (midi-note &optional (scale cm::*scale*))
;;; ****
  ;; (degree-to-note midi-note cm::*chromatic-scale*))
  (degree-to-note (midi-to-degree midi-note scale) scale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* cm/midi-to-freq
;;; DESCRIPTION
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
;;; ****f* cm/note-to-midi
;;; DESCRIPTION
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
;;; ****f* cm/degrees-to-notes
;;; DESCRIPTION
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
;;; DESCRIPTION
;;; Get the note-name pitch equivalent of the specified frequency, rounded to
;;; the nearest scale degree of the current scale.
;;; 
;;; ARGUMENTS
;;; A number that is a frequency in Hertz.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The scale in which the note-name pitch equivalent is to be sought (Common
;;; Music scale object or symbol). If a symbol, then 'chromatic-scale,
;;; 'twelfth-tone, or 'quarter-tone only at present.
;;; 
;;; RETURN VALUE
;;; A note-name pitch symbol.
;;; 
;;; EXAMPLE
#|
(freq-to-note 423 'chromatic-scale)

=> AF4

(freq-to-note 423 'twelfth-tone)

=> GSSS4

(freq-to-note 423 'quarter-tone)

=> AQF4

|#
;;; SYNOPSIS
(defun freq-to-note (freq &optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (find-scale scale)))
  ;; (print scale)
  (rm-package (cm::note freq :hz t :in scale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* cm/note-to-freq
;;; DESCRIPTION
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
;;; DESCRIPTION
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
(note-to-degree 'AF4 'chromatic-scale)

=> 68

(note-to-degree 'AF4 'twelfth-tone)

=> 408

(note-to-degree 'AF4 'quarter-tone)

=> 136

|#
;;; SYNOPSIS
(defun note-to-degree (note &optional (scale cm::*scale*))
;;; ****
  ;; MDE Mon May 14 21:01:10 2012 -- 
  (when (and scale (symbolp scale))
    (setf scale (find-scale scale)))
  (cm::keynum (rm-package note :cm) :in scale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 20:01:30 BST 2012: Added robodoc entry

;;; Defaults to *scale*

;;; ****f* cm/freq-to-degree
;;; DESCRIPTION
;;; Get the scale degree of the specified frequency in Hertz within the current
;;; scale. 
;;; 
;;; NB: This method will return fractional scale degrees.
;;; 
;;; ARGUMENTS
;;; A frequency in Hertz.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The scale in which to find the corresponding scale degree.
;;; 
;;; RETURN VALUE
;;; A scale degree number. This may be a decimal number.
;;; 
;;; EXAMPLE
#|
(freq-to-degree 423 'chromatic-scale)

=> 68.317856

(freq-to-degree 423 'twelfth-tone)

=> 409.9071

(freq-to-degree 423 'quarter-tone)

=> 136.63571

|#
;;; SYNOPSIS
(defun freq-to-degree (freq &optional (scale cm::*scale*))
;;; ****
  (cm::keynum freq :hz t :in (find-scale scale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* cm/freq-to-midi
;;; DATE
;;; 10th July 2016, Essen Werden
;;; 
;;; DESCRIPTION
;;; convert a frequency in Hertz to a MIDI note number (possibly floating
;;; point, indicating microtonality).  
;;; 
;;; ARGUMENTS
;;; a frequency in Hertz
;;; 
;;; RETURN VALUE
;;; a floating point value representing the MIDI note number of the given frequency
;;; 
;;; EXAMPLE
#|
(freq-to-midi 260) --> 59.892094
(freq-to-midi (midi-to-freq 60)) --> 60.0
|#
;;; SYNOPSIS
(defun freq-to-midi (freq)
;;; ****  
  (freq-to-degree freq cm::*chromatic-scale*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri May  4 20:08:11 BST 2012: Added robodoc entry

;;; MDE Sat Jan  7 18:06:01 2012 -- this will always return >= 0 <= 1.0 so a
;;; bend upwards from the nearest chromatic note below our freq 

;;; ****f* cm/get-pitch-bend
;;; DESCRIPTION
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
  (let* ((chromatic-degree (freq-to-degree freq 'chromatic-scale))
         (rem (rem chromatic-degree 1)))
    ;; float discrepancies result in e.g. c#4 being 60.999996....
    (if (or (equal-within-tolerance 1 rem .005) ; used to be .0001
            (equal-within-tolerance 0 rem .005))
        0.0
        ;; rem)))
        ;; MDE Sat Jan  7 00:39:10 2012 -- instead of the above try
        ;; rounding to 0.01 (1 cent) 
        (/ (round rem 0.01) 100.0))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;;; obsolete
(defun list-midi-events (file)
  (cm::list-objects (cm::import-events file)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun integer-as-string (string start) ; 0-based
  (when (> (length string) start)
    (integerp (read-from-string (subseq string start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-qtr-flat (note)
  ;; MDE Mon Oct 21 08:14:44 2024, Heidhausen -- different logic: see below
  (when (cm::note note)
    (search "QF" (string note)))) ;(cm::note (rm-package note :cm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-qtr-sharp (note)
  (when (cm::note note)
    (search "QS" (string note )))) ;(cm::note (rm-package note :cm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-sharp (note)
  ;; MDE Sat Oct 19 14:50:29 2024, Heidhausen -- no need for cm note in the
  ;; string
  (when (cm::note note)
    (let ((str (string note)))          ;(cm::note (rm-package note :cm)))))
      (and (equal #\S (elt str 1))
           ;; (digit-char-p (elt str 2)))
           ;; MDE Sun Dec 29 14:19:55 2013 -- got to take octave -1 into
           ;; account!  
           (integer-as-string str 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-flat (note)
  ;; MDE Sat Oct 19 14:47:43 2024, Heidhausen -- not need to use the cm fun for
  ;; making the string--problematic as e.g. es4 is turned into f4 so (sharp ) is
  ;; nil when turned into a pitch--as before but good to check that we've been
  ;; passed an actual note symbol
  (when (cm::note note)
    (let ((str (string note)))            
      ;; (print str)
      (and (equal #\F (elt str 1))
           ;; (digit-char-p (elt str 2)))
           ;; MDE Sun Dec 29 14:19:55 2013 -- got to take octave -1 into
           ;; account!  
           (integer-as-string str 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B. won't work with bracketed accidentals of the form cbn3!

(defun is-natural (note)
  (when (cm::note note)
    (let* ((string (string note))
           2nd-char)
      (when (> (length string) 1)
        (setf 2nd-char (elt string 1))
        ;; MDE Sun Dec 29 14:19:55 2013 -- got to take octave -1 into
        ;; account!  
        ;; (or (numberp (digit-char-p 2nd-char))
        ;;    (equal 2nd-char #\N)))))
        (or (integer-as-string string 1)
            (and (equal 2nd-char #\N)
                 (integer-as-string string 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 11:31:58 BST 2012: Added robodoc entry

;;; MDE Sat Feb 11 10:41:11 2012 

;;; ****f* cm/degrees-per-semitone
;;; DESCRIPTION
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
(degrees-per-semitone 'chromatic-scale)

=> 1

(degrees-per-semitone 'twelfth-tone)

=> 6

(degrees-per-semitone 'quarter-tone)

=> 2

|#
;;; SYNOPSIS
(defun degrees-per-semitone (&optional (scale cm::*scale*))
;;; ****                                
  (let ((scale-obj scale))
    ;; MDE Mon May 14 21:01:10 2012 --    
    (when (and scale (symbolp scale))
      (setf scale-obj (find-scale scale))
      ;; MDE Fri Jun 15 14:46:57 2012 --  
      (unless scale-obj
        (warn "cm::degrees-per-semitone: can't find scale ~a" scale)))
    (- (note-to-degree 'cs4 scale-obj)
       (note-to-degree 'c4 scale-obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-note (note semitones &optional (package nil))
  (when (and note semitones)
    (let ((n note))
      (when (listp n) ;; could be (c1 natural)
        (setf n (first n)))
      (setf n (force-octave (rm-package n :sc)))
      (let* ((degrees-per-semitone (degrees-per-semitone))
             ;; MDE Tue Mar 29 12:30:49 2022, Heidhausen -- round, not floor!
             ;; (degrees (floor (* semitones degrees-per-semitone)))
             (degrees (round (* semitones degrees-per-semitone)))
             (degree (note-to-degree n)))
        ;; (print degrees)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if no octave given in note use the last octave and return a symbol that
;;; includes it 
(defun force-octave (note)
  (multiple-value-bind
        (n o)
      (get-note-octave note t)
    (join-note-octave n o)))
  
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
                        (read-from-string string t nil :start 0 
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
;;; errors and your MIDI file was exported from e.g. Sibelius, it could be that
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
       ;; so that we calculate how many bars have passed.  The calculation
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
;;; nothing more than an alias to the same function really in cm package in
;;; cm-cm.lsp 
;;; ****f* cm/event-list-to-midi-file
;;; DESCRIPTION
;;; Write the events in a list to a midi-file. events-update-time is a related
;;; function useful for preparind ad-hoc events for midi-file-writing. 
;;; 
;;; ARGUMENTS
;;; - A list of events objects
;;; - the path to the midi-file
;;; - the starting tempo (integer: BPM)
;;; - a time-offset for the events (seconds)
;;; 
;;; OPTIONAL ARGUMENTS
;;; - whether to overwrite events' amplitude slots and use a single
;;; velocity/amplitude value given here (0-1.0 (float) or 0-127 (integer) 
;;; 
;;; RETURN VALUE
;;; The MIDI file path
;;; 
;;; SYNOPSIS 
(defun event-list-to-midi-file (event-list 
                                &key (midi-file "/tmp/tmp.mid")
                                  (start-tempo 120) (time-offset 0)
                                  (auto-open (get-sc-config
                                              'midi-play-auto-open))
                                  force-velocity)
;;; ****
  (cm::event-list-to-midi-file event-list midi-file start-tempo time-offset
                               force-velocity)
  (when auto-open
    (system-open-file midi-file))
  midi-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* cm/midi-file-to-events
;;; DATE
;;; 28/6/16, Edinburgh
;;; 
;;; DESCRIPTION
;;; Read in a MIDI file and convert notes to event objects.
;;; NB This won't (yet) import microtones as indicated by pitch-bend messages
;;; 
;;; ARGUMENTS
;;; the path to the midi file
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :track. The track number to read. Default = NIL which means read all
;;; tracks.
;;; - :tempo. The tempo of the track in crotchets (quarter notes) per
;;; minute. This should override the midi-file's tempo.
;;; 
;;; RETURN VALUE
;;; a list of event objects
;;; 
;;; SYNOPSIS 
(defun midi-file-to-events (file &key track tempo)
;;; ****
  (let* ((cm-midi (cm::parse-midi-file file track tempo))
         (tmpo (when tempo (make-tempo tempo)))
         ;; (start-qtrs 0.0)
         (tempo-change nil)
         (result '()))
    ;; 
    ;; (print (subseq cm-midi 0 50))
    (loop for m in cm-midi do
      (typecase m
        (cm::midi (let* ((dur (cm::midi-duration m))
                         (e (unless (zerop dur)
                              (make-event (midi-to-note (cm::midi-keynum m))
                                          dur :duration t))))
                                          ;;:tempo (bpm tmpo)))))
                    ;; (print e)
                    (when e
                      ;; assume last change was on this chan
                      (when tempo-change 
                        (setf (tempo-change e) tmpo
                              (display-tempo e) t
                              tempo-change nil))
                      ;; (print (cm::object-time m))
                      (setf (amplitude e) (cm::midi-amplitude m)
                            ;; MDE Mon May 19 19:51:47 2025, Heidhausen -- the
                            ;; time and duration slots of cm::midi are seconds,
                            ;; not quarter notes or ticks 
                            (start-time e) (cm::object-time m)
                            ;; (start-time-qtrs e) start-qtrs
                            ;; todo: it's not as simple as this
                            ;; (start-time-qtrs e) (/ (start-time e)
                               ;;                    (beat-dur tmpo))
                            (duration-in-tempo e) dur
                            ;; (* (duration e) (qtr-dur tmpo))
                            (compound-duration e) dur
                            (compound-duration-in-tempo e)
                            ;; (duration-in-tempo e))
                            dur)
                      (set-midi-channel e (1+ (cm::midi-channel m)))
                      ;; (incf start-qtrs (duration e))
                      (push e result))))
        (cm::midi-tempo-change
           (setq tempo-change t
                 ;;                 that's the usecs slot
                 tmpo (make-tempo (cm::midi-event-data1 m))))))
    ;; (nreverse result))) 
    ;; result is in reverse order but the following function will effectively
    ;; reverse for us so save some consing.
    (midi-file-to-events-handle-chords result nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this assumes all events are single pitches. 
(defun midi-file-to-events-handle-chords (events &optional (reverse t))
  (let ((last (first events))
        result)
    (loop for e in (rest events) do
      ;; tolerance of 1 millisec which is OK for midi files generated by
      ;; sequencers etc. but might not be enough wiggle room if played in by
      ;; humans: if it crops up make this .001 an optional arg.
      ;;
      ;; have to consider durations here too: if two notes played
      ;; together but one is longer than the other, then it shouldn't be a
      ;; chord 
      (if (and (equal-within-tolerance (start-time last) (start-time e) .001)
               (equal-within-tolerance (compound-duration-in-tempo last)
                                       (compound-duration-in-tempo e) .001))
        (add-pitches last (pitch-or-chord e))
        (progn
          (push last result)
          (setq last e)))
      ;; MDE Sat Jun 18 10:18:54 2022, Heidhausen -- doh! was forgetting to get
      ;; the first event :/
          finally (push last result))
    (if reverse (nreverse result) result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* cm/midi2qlist
;;; DATE
;;; 10th November 2016, Edinburgh
;;; 
;;; DESCRIPTION
;;; Convert a midi-file to qlist text format, for sequencing in PD or MaxMSP.
;;; If you don't want specific tracks from the file, just pass two arguments so
;;; <tracks> remains nil.
;;; 
;;; ARGUMENTS
;;; - the path to the midi file you want to convert (string)
;;; - the path to the qlist text file you'd like to write (string). If this is
;;;   nil then we'll write to the same patch as the midi file with the extension
;;;   .txt added. NB If this file exists it will be overwritten, no questions
;;;   asked.
;;; - (&rest) the numbers of the tracks you'd like to convert, starting from 1.
;;; 
;;; RETURN VALUE
;;; The number of notes written (integer)
;;; 
;;; SYNOPSIS
(defun midi2qlist (midi-file qlist-file &rest tracks)
;;; ****
  (unless qlist-file
    (setq qlist-file (concatenate 'string midi-file ".txt")))
  (let* ((events (if tracks
                     (loop for tr in tracks appending
                          (midi-file-to-events midi-file :track tr))
                     (midi-file-to-events midi-file))))
    ;; there might be some nil events so remove them then intermingle the
    ;; tracks, sorting by start time (might as well do this even when there are
    ;; no tracks as who knows...)
    (setq events (sort (remove-if #'not events)
                       #'(lambda (x y)
                           (< (start-time x) (start-time y)))))
    (with-open-file (qlist qlist-file :direction :output
                           :if-does-not-exist :create
                           :if-exists :overwrite)
      (loop for e in events with last-time = 0.0 do
         ;; qlist line format is the delay before the message is sent; the
         ;; receiver; the data; a semi-colon at the end of the line
           (format qlist "~&~,3f qlmidinote ~a ~a;"
                   ;; delay is in millisecs
                   (* 1000.0 (-  (start-time e) last-time))
                   (midi-note (pitch-or-chord e))
                   (floor (* 127.0 (amplitude e))))
           (setf last-time (start-time e))))
    (length events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cm.lsp

