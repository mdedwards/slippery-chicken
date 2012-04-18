;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/instruments
;;; NAME 
;;; instrument
;;;
;;; File:             instruments.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          0.9.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of various standard instruments and other
;;;                   data/functions useful to slippery-chicken users. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    30th December 2010
;;;
;;; $$ Last modified: 16:23:27 Wed Apr 18 2012 BST
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

;;; ****P* instruments/+slippery-chicken-standard-instrument-palette+
;;; NAME
;;; +slippery-chicken-standard-instrument-palette+:
;;;
;;; A palette of standard instruments (by no means exhaustive...) for use
;;; directly in projects or for combining with user palettes e.g.
;;; 
;;; (combine
;;;  +slippery-chicken-standard-instrument-palette+
;;;  (make-instrument-palette
;;;   'esoteric-stuff
;;;    '((toy-piano
;;;       (:staff-name "toy piano" ....
;;; 
;;; SYNOPSIS
(defparameter +slippery-chicken-standard-instrument-palette+
  (make-instrument-palette
   'slippery-chicken-standard-instrument-palette
   ;; SAR Fri Jan 20 11:43:32 GMT 2012: Re-ordering these to Adler's "standard"
   ;; score order for easier look-up
   '((piccolo
      (:staff-name "piccolo" :staff-short-name "picc"
       :lowest-written d4 :highest-written c7 :transposition-semitones 12 
       :missing-notes nil 
       :starting-clef treble
       :chords nil 
       :microtones t 
       :midi-program 73))
     (flute 
      (:staff-name "flute" :staff-short-name "fl" 
       :lowest-written c4 :highest-written d7 
       :missing-notes (cqs4 dqf4) 
       :starting-clef treble
       :chords nil 
       :microtones t 
       :midi-program 74))
     (alto-flute 
      (:staff-name "alto flute" :staff-short-name "alt fl"
       :lowest-written c4 :highest-written c7 :transposition-semitones -5
       :missing-notes (cqs4 dqf4) 
       :starting-clef treble
       :chords nil 
       :microtones t 
       :midi-program 74))
     ;; SAR Fri Jan 20 11:46:45 GMT 2012: Modified bass flute range to that
     ;; stated by Adler.
     (bass-flute 
      (:staff-name "bass flute" :staff-short-name "bass fl" 
       :lowest-written c4 :highest-written c7 :transposition-semitones -12
       :missing-notes (cqs4 dqf4) 
       :clefs-in-c '(treble bass) :starting-clef treble 
       :chords nil 
       :microtones t 
       :midi-program 74))
     ;; SAR Fri Jan 20 12:01:37 GMT 2012: Added oboe. Conservative range taken
     ;; from the Adler
     (oboe
      (:staff-name "oboe" :staff-short-name "ob" 
       :lowest-written bf3 :highest-written a6
       :starting-clef treble 
       :chords nil 
       :midi-program 69))
     (e-flat-clarinet 
      (:staff-name "E-flat clarinet" :staff-short-name "E-flat cl"
       :lowest-written e3 :highest-written a6 :transposition-semitones 3
       :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3)
       :starting-clef treble
       :chords nil 
       :microtones t 
       :midi-program 72))
     (b-flat-clarinet 
      (:staff-name "B-flat clarinet" :staff-short-name "B-flat cl"
       :lowest-written e3 :highest-written a6 :transposition-semitones -2 
       :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3) 
       :starting-clef treble 
       :chords nil 
       :microtones t 
       :midi-program 72))
     (a-clarinet 
      (:staff-name "A clarinet" :staff-short-name "A cl"
       :lowest-written e3 :highest-written a6 :transposition-semitones -3
       :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3)
       :starting-clef treble 
       :chords nil 
       :microtones t
       :midi-program 72))
     (bass-clarinet 
      (:staff-name "bass clarinet" :staff-short-name "bass cl"
       :lowest-written c3 :highest-written g6 :transposition-semitones -14
       :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3 eqf3 dqs3 dqf3
                            cqs3) 
       :prefers-notes low       
       :clefs (treble) :clefs-in-c (treble bass) :starting-clef treble 
       :chords nil 
       :microtones t
       :midi-program 72))
     (soprano-sax 
      (:staff-name "soprano saxophone" :staff-short-name "sop sax"
       :lowest-written bf3 :highest-written fs6 :transposition-semitones -2
       :missing-notes (gqs4 gqs5)
       :starting-clef treble 
       :chords nil
       :microtones t 
       :midi-program 65))
     (alto-sax 
      (:staff-name "alto saxophone" :staff-short-name "alt sax"
       ;; altissimo extra....by hand...
       :lowest-written bf3 :highest-written fs6 :transposition-semitones -9
       :missing-notes (gqs4 gqs5)
       :starting-clef treble 
       :chords nil 
       :microtones t 
       :midi-program 66))
     (tenor-sax 
      (:staff-name "tenor sax" :staff-short-name "ten sax"
       :lowest-written bf3 :highest-written fs6 :transposition-semitones -14 
       :missing-notes (gqs4 gqs5) 
       :starting-clef treble :clefs-in-c '(treble bass)
       :chords nil 
       :microtones t 
       :midi-program 67))
     (baritone-sax 
      (:staff-name "baritone sax" :staff-short-name "bar sax"
       :lowest-written bf3 :highest-written fs6 :transposition-semitones -21 
       :missing-notes (gqs4 gqs5)
       :clefs-in-c '(treble bass) :starting-clef treble 
       :chords nil
       :microtones t 
       :midi-program 68))
     (bassoon 
      (:staff-name "bassoon" :staff-short-name "bsn" 
       ;; of course it can go higher but best not to algorithmically select
       ;; these   
       :lowest-written bf1 :highest-written c5 
       ;; Wolfgang Ruediger says all 1/4 tones are OK above low E
       :missing-notes (bqf1 bqs1 cqs2 dqf2 dqs2 eqf2)
       :largest-fast-leap 13 
       :clefs (bass tenor) :starting-clef bass 
       :chords nil
       :microtones t
       :midi-program 71))
     (french-horn
      (:staff-name "french horn" :staff-short-name "hn" 
       :lowest-written c3 :highest-written c6 :transposition-semitones -7 
       :clefs (treble bass) :starting-clef treble
       :chords nil 
       :microtones t
       :midi-program 61))
     (c-trumpet
      (:staff-name "trumpet in c" :staff-short-name "c tpt" 
       :lowest-written fs3 :highest-written c6
       :clefs (treble) :starting-clef treble 
       :chords nil 
       :microtones t
       :midi-program 57))
     ;; SAR Fri Jan 20 12:09:41 GMT 2012: Added b-flat-trumpet from Adler
     ;;; MDE Mon Feb 20 20:02:55 2012 -- modified to keep in line with clarinet
     (b-flat-trumpet
      (:staff-name "B-flat trumpet" :staff-short-name "b-flat tpt" 
       ;; the -flat should be convereted in CMN and Lilypond to the flat sign
       :lowest-written fs3 :highest-written d6 :transposition-semitones -2 
       :starting-clef treble 
       :chords nil 
       :midi-program 57))
     ;; SAR Fri Jan 20 12:17:24 GMT 2012: Added tenor trombone from Adler
     (tenor-trombone
      (:staff-name "trombone" :staff-short-name "tbn" 
       :lowest-written e2 :highest-written bf4
       :clefs (bass tenor) :starting-clef bass 
       :chords nil 
       :midi-program 58))
     (vibraphone 
      (:staff-name "vibraphone" :staff-short-name "vib"
       :lowest-written f3 :highest-written f6 
       :starting-clef treble 
       :chords t 
       :microtones nil
       :midi-program 12))
     (marimba 
      (:staff-name "marimba" :staff-short-name "mba"
       :lowest-written c3 :highest-written c7 
       :starting-clef treble :clefs (treble) ; (treble bass) 
       :chords t
       :microtones nil
       :midi-program 13))
     (piano
      (:staff-name "piano" :staff-short-name "pno"
       :lowest-written a0 :highest-written c8 
       :largest-fast-leap 9
       :clefs (treble bass double-treble double-bass) :starting-clef treble
       :chords t :chord-function piano-chord-fun
       :microtones nil 
       :midi-program 1))
     ;; we generally treat the piano as two instruments (LH, RH), generating
     ;; lines separately.  So this is the same as the piano instrument but has
     ;; no staff-name and starts with bass clef.  Use set-limits to change the
     ;; range of the two hands, as they're both set to be full piano range
     ;; here.
     (piano-lh
      (:lowest-written a0 :highest-written c8 
       :largest-fast-leap 9 
       :chords t :chord-function piano-chord-fun
       :clefs (treble bass double-treble double-bass) :starting-clef bass 
       :microtones nil 
       :midi-program 1))
     (tambourine
      (:staff-name "tambourine" :staff-short-name "tmb"
       :lowest-written c4 :highest-written c4
       :starting-clef percussion
       :midi-program 1))
     (guitar 
      (:staff-name "guitar" :staff-short-name "gtr"
       :lowest-written e3 :highest-written b6 :transposition-semitones -12 
       :largest-fast-leap 31
       :starting-clef treble 
       :chords t :chord-function guitar-chord-selection-fun 
       :microtones nil
       :midi-program 28))
     (soprano
      (:staff-name "soprano" :staff-short-name "s"
       :lowest-written c4 :highest-written c6
       :starting-clef treble
       :midi-program 54))
     (violin 
      (:staff-name "violin" :staff-short-name "vln"
       :lowest-written g3 :highest-written c7 
       :largest-fast-leap 13
       :starting-clef treble 
       :chords t :chord-function violin-chord-selection-fun 
       :microtones t
       :midi-program 41))
     (viola 
      (:staff-name "viola" :staff-short-name "vla"
       :lowest-written c3 :highest-written f6 
       :largest-fast-leap 13 
       :clefs (alto treble) :starting-clef alto 
       :chords t :chord-function viola-chord-selection-fun 
       :microtones t
       :midi-program 42))
     (viola-d-amore 
      (:staff-name "viola d'amore" :staff-short-name "vla d'am"
       :lowest-written a2 :highest-written f7 
       :largest-fast-leap 13
       :clefs (alto treble) :starting-clef alto 
       :chords t :chord-function nil 
       :microtones t
       :midi-program 41))
     (cello 
      (:staff-name "cello" :staff-short-name "vc"
       ;; of course it can go higher but best not to algorithmically select 
       ;; these
       :lowest-written c2 :highest-written a5 
       :largest-fast-leap 12
       :clefs (bass tenor treble) :starting-clef bass
       :chords t :chord-function cello-chord-selection-fun 
       :microtones t 
       :midi-program 43))
     (double-bass 
      (:staff-name "double bass" :staff-short-name "db"
       :lowest-written e2 :highest-written g5 :transposition-semitones -12 
       :prefers-notes low
       :largest-fast-leap 10
       :clefs (bass tenor treble) :starting-clef bass
       :chords nil 
       :microtones t 
       :midi-program 44))
     ;; SAR Thu Apr 12 18:19:21 BST 2012: Added "computer" part for "silent"
     ;; parts in case the user would like to create rhythmically independent
     ;; computer parts.
     (computer
      (:staff-name "computer" :staff-short-name "comp"
       :starting-clef percussion)))))
       ;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Tue Mar 20 15:55:39 2012 -- add some more default chord functions for
;;; the user to choose from.

;;; skip every other note, go for three notes, max one octave apart
;;; ****f* instruments/chord-fun1
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
(defun chord-fun1 (curve-num index pitch-list pitch-seq instrument set)
;;; ****
  (chord-fun-aux curve-num index pitch-list pitch-seq instrument set 2 3 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; every third note, 4 notes, (almost) no limit to span
;;; ****f* instruments/chord-fun2
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
(defun chord-fun2 (curve-num index pitch-list pitch-seq instrument set)
  (chord-fun-aux curve-num index pitch-list pitch-seq instrument set 3 4 999))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remember that the index is the desired top note of the chord
;;; ****f* instruments/chord-fun-aux
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
(defun chord-fun-aux (curve-num index pitch-list pitch-seq instrument set
                      skip num-notes max-span)
;;; ****
  (declare (ignore set instrument pitch-seq curve-num))
  (unless (and (integer>0 skip) (integer>0 num-notes) (integer>0 max-span))
    (error "slippery-chicken::instruments:: skip, num-notes, and max-span must
            be integers > 0"))
  (let* ((start (max 0 (- index (- (* skip num-notes) skip))))
         (at-start (nth start pitch-list))
         (result (list at-start)))
    (loop 
       repeat num-notes
       for i from start by skip
       for p = (nth i pitch-list)
       do
         ;; (print (data p))
         (when (and p (<= (pitch- p at-start) max-span)
                    (not (member p result :test #'note=)))
           (push p result)))
    (if (> (length result) 1)
        (make-chord result)
        (first result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following chord selection functions were designed for my piece cheat
;;; sheet but they may well be useful elsewhere.

(defun best-string-diad (pitch-list)
  ;; pitch list is either ascending or descending
  (let* ((p1 (first pitch-list))
         ;; (rest-sorted (sort (rest pitch-list) #'pitch<))
         (possible (loop for p in (rest pitch-list) 
                      for diff = (abs (pitch- p p1))
                        do
                      ;; must be above a perfect fifth, and let's avoid
                      ;; microtonal chords for ease of playing
                      (when (and (not (micro-tone p))
                                (>  diff 7)
                                (<= diff 11))
                        (return p)))))
    (when possible
      (make-chord (list p1 possible)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* instruments/string-chord-selection-fun
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
(defun string-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                   instrument set string-III)
;;; ****
  (let* ((pll (length pitch-list))
         (diad-down (best-string-diad 
                     ;; have to reverse so that pitch at index is our starting
                     ;; point  
                     ;; MDE Sat Mar 31 09:51:32 2012 -- index could be > length 
                     (reverse (subseq pitch-list 0 (min pll (1+ index))))))
         (diad-up (unless diad-down
                    (best-string-diad (subseq pitch-list (min index pll)))))
         (default (unless (or diad-down diad-up)
                    (default-chord-function curve-num index pitch-list
                                            pitch-seq instrument set)))
         (diad (cond (diad-down diad-down)
                     (diad-up diad-up)
                     (t default)))
         (high (when (> (sclist-length diad) 1)
                 (second (data diad)))))
    (if (or (micro-tone diad)           ; default could be microtonal
            (and high
                 ;; can't have any 2-note chords where highest note is < open
                 ;; III 
                 (pitch< high string-III)))
        (if high
            ;; just return the highest note
            high
            (first (data diad)))
        diad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* instruments/violin-chord-selection-fun
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
(let ((vln-III (make-pitch 'd4)))
  (defun violin-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                     instrument set)
;;; **** 
    (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                                instrument set vln-III)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* instruments/viola-chord-selection-fun
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
(let ((vla-III (make-pitch 'g3)))
  (defun viola-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                    instrument set) 
;;; **** 
    (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                                instrument set vla-III)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* instruments/cello-chord-selection-fun
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
(let ((vc-III (make-pitch 'g2)))
  (defun cello-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                    instrument set)
;;; **** 
    (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                                instrument set vc-III)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* instruments/piano-chord-fun
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
(defun piano-chord-fun (curve-num index pitch-list pitch-seq instrument set)
;;; ****
  (declare (ignore set instrument pitch-seq curve-num))
  (let* ((start (max 0 (- index 3))) ;; try to get 4 notes
         (at-start (nth start pitch-list))
         (result (list at-start)))
    (loop 
       for i from (1+ start) to (+ start 3) 
       for p = (nth i pitch-list)
       do
         (when (and p (<= (pitch- p at-start) 12)
                    (not (member p result :test #'note=)))
           (push p result)))
    (if (> (length result) 1)
        (make-chord result)
        (first result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns the index into the pitch-list of the first note playable on an
;; open-string (pitch object) and the fret number of the first playable note
(defun guitar-chord-get-first-note (open-string pitch-list)
  (loop for p in pitch-list and i from 0 do
       (when (pitch>= p open-string)
         (return (values i (round (midi- p open-string)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; string is an index (0-based) into tuning-dist (which is a list of semitone
;;; distances between lowest and higher strings (lowest is 0)
;;; fret is an int where 0 is open:
;;; this indicates the position of the first playable note on <string> from
;;; pitch-list: this is also the note at pitch-list-index
;;; nb this assumes we're not playing <string> open
(defun guitar-chord-from-arbitrary-pitches-aux 
    (pitch-list pitch-list-index tuning tuning-dist string fret)
  (declare (ignore fret))
  (let* ((one (nth pitch-list-index pitch-list))
         (others (loop with str-offset = (- (nth string tuning-dist))
                    for f from 1 to 3
                    for str from (1+ string) below (length tuning-dist)
                    for open = (nth str tuning)
                    for next = (pitch-inc one (+ f str-offset 
                                                 (nth str tuning-dist)))
                    if (member next pitch-list :test #'note=)
                    collect next into result
                    else if (member open pitch-list :test #'note=)
                    collect open into result
                    else do (return result)
                    ;; do (format t "~&str ~a fret ~a" str f)
                    finally (return result)))
         (result (cons one others)))
    (setf result (loop for n in result collect (data n)))
    result))
       
;;; see default-chord-function above for description of arguments.
(defun guitar-chord-from-arbitrary-pitches (curve-num index pitch-list
                                            pitch-seq instrument set)
  (declare (ignore set instrument pitch-seq index curve-num))
  (let* ((tuning (loop for n in '(e2 a2 d3 g3 b3 e4) collect (make-pitch n)))
         (tuning-dist (cons 0 (loop with lowest = (first tuning)
                                 for p in (rest tuning)
                                 collect (pitch- p lowest))))
         (poss '()))
    (loop with temp for open in (butlast tuning) and i from 0 do
         (multiple-value-bind
               (pindex fret)
             (guitar-chord-get-first-note
              open pitch-list)
           (setf temp (when pindex
                        (guitar-chord-from-arbitrary-pitches-aux 
                         pitch-list pindex tuning tuning-dist i fret)))
           ;; or just collect all including nil? at least then they're sorted
           ;; by string 
           (when (>= (length temp) 2)
             (push temp poss))))
    (nreverse poss)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NB this assumes the pitch-list contains notes that are already playable as
;;; a guitar chord (which was fine in cheat sheet, as that was the premise of
;;; our harmony).

;;; ****f* instruments/guitar-chord-selection-fun
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
(let ((last-chord '()))
  (defun guitar-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                     instrument set)
;;; **** 
    (declare (ignore index instrument pitch-list))
    (let* ((subset (get-data 'guitar (subsets set)))
           (subset-pitches (data subset))
           (tag (tag subset))
           ;; find out where the current number from the pitch curve is
           ;; relative to the lowest/highest in the curve; using this we can
           ;; decide whether to play all or only some of the 6-note guitar
           ;; chord.
           (range (1+ (- (highest pitch-seq) (lowest pitch-seq))))
           (tessitura (/ curve-num range))
           (strings '(VI V IV III II I))
           nth1 nth2 fingering notes chord show-fingering)
      (cond ((< tessitura 0.3) (setf nth1 0 nth2 3))
            ((< tessitura 0.5) (setf nth1 0 nth2 4))
            ((< tessitura 0.7) (setf nth1 1 nth2 5))
            ((>= tessitura 0.7) (setf nth1 0 nth2 6)))
      (setf notes (subseq subset-pitches nth1 nth2)
            fingering (subseq tag nth1 nth2)
            strings (subseq strings nth1 nth2)
            show-fingering (not (equal (pitch-list-to-symbols last-chord)
                                       (pitch-list-to-symbols notes)))
            last-chord (copy-list notes)
            chord (make-chord notes))
      (add-mark chord (when show-fingering
                        (apply #'cmn::fingering 
                               (append
                                (reverse 
                                 (loop 
                                    for s in strings 
                                    for f in fingering 
                                    collect
                                    (format nil "~3a ~a" s f)))
                                ;;'(:direction :up)))))
                                ))))
      chord)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF instruments.lsp
