;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/instruments
;;; NAME 
;;; instrument
;;;
;;; File:             instruments.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          1.0
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
;;; $$ Last modified: 13:36:35 Fri Dec  9 2011 ICT
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
;;; todo: add more/all known instruments
(defparameter +slippery-chicken-standard-instrument-palette+
 (make-instrument-palette
  'slippery-chicken-standard-instrument-palette
   '((piano
      (:staff-name "piano" :lowest-written a0 :highest-written c8 :chords t
       ;;; ****
       :midi-program 1 :starting-clef treble :staff-short-name "pno"
       :clefs (treble bass double-treble double-bass)
       :microtones nil :largest-fast-leap 9 :chord-function piano-chord-fun))
     ;; this is a bit of a hack but: we generally treat the piano as two
     ;; instruments (LH, RH), generating lines separately, as if for two
     ;; instruments.  So this is the same as the piano instrument but has no
     ;; staff-name and starts with bass clef.  Use set-limits to change the
     ;; range of the two hands, as they're both set to be full piano range
     ;; here. 
     (piano-lh
      (:lowest-written a0 :highest-written c8 :chords t
       :midi-program 1 :starting-clef bass 
       :clefs (treble bass double-treble double-bass)
       :microtones nil :largest-fast-leap 9 :chord-function piano-chord-fun))
     (guitar 
      (:staff-name "guitar" :lowest-written e3 :staff-short-name "gtr"
       :highest-written b6 :chords t :midi-program 28 :starting-clef treble 
       :transposition-semitones -12 :microtones nil
       :chord-function guitar-chord-selection-fun :largest-fast-leap 31))
     (piccolo
      (:staff-name "piccolo" :lowest-written d4 :highest-written c7 :chords nil
       :staff-short-name "picc"
       :missing-notes nil :midi-program 73 :starting-clef treble 
       :transposition-semitones 12 :microtones t))
     (flute 
      (:staff-name "flute" :lowest-written c4 :highest-written d7 :chords nil 
       :missing-notes (cqs4 dqf4) :midi-program 74 :starting-clef treble
       :staff-short-name "fl" :microtones t))
     (alto-flute 
      (:staff-name "alto flute" :lowest-written c4 :highest-written c7
       :staff-short-name "alt fl"
       :chords nil :missing-notes (cqs4 dqf4) :midi-program 74 
       :starting-clef treble :microtones t :transposition-semitones -5))
     ;; todo: check real range of bass flute
     (bass-flute 
      (:staff-name "bass flute" :lowest-written c4 :highest-written a6
       :staff-short-name "bass fl" :clefs-in-c '(treble bass)
       :chords nil :missing-notes (cqs4 dqf4) :midi-program 74 
       :starting-clef treble :microtones t :transposition-semitones -12))
     (bass-clarinet 
      (:staff-name "bass clarinet" :lowest-written c3 :highest-written g6
       :staff-short-name "bass cl"
       :chords nil :midi-program 72 :starting-clef treble :microtones t
       :prefers-notes low
       :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3 eqf3 dqs3 dqf3 
                            cqs3)
       :clefs (treble) :clefs-in-c (treble bass) :transposition-semitones -14)) 
     (e-flat-clarinet 
      (:staff-name "Eflat clarinet" :lowest-written e3 :highest-written a6
       :staff-short-name "Eflat cl"
       :chords nil :midi-program 72 :starting-clef treble :microtones t
       :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3)
       :transposition-semitones 3)) 
     (b-flat-clarinet 
      (:staff-name "Bflat clarinet" :lowest-written e3 :highest-written a6
       :staff-short-name "Bflat cl"
       :chords nil :midi-program 72 :starting-clef treble :microtones t
       :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3)
       :transposition-semitones -2)) 
     (a-clarinet 
      (:staff-name "A clarinet" :lowest-written e3 :highest-written a6
       :staff-short-name "A cl"
       :chords nil :midi-program 72 :starting-clef treble :microtones t
       :missing-notes (aqs4 bqf4 bqs4 cqs5 dqf5 gqf3 fqs3 fqf3)
       :transposition-semitones -3)) 
     (soprano-sax 
      (:staff-name "soprano saxophone" :lowest-written bf3
       :staff-short-name "sop sax"
       :highest-written fs6 :starting-clef treble :chords nil
       :midi-program 65 :microtones t :missing-notes (gqs4 gqs5)
       :transposition-semitones -2))
     (alto-sax 
      (:staff-name "alto saxophone" :lowest-written bf3
       :staff-short-name "alt sax"
       :highest-written fs6 ;; altissimo extra....by hand...
       :starting-clef treble :chords nil :midi-program 66 
       :microtones t :missing-notes (gqs4 gqs5)
       :transposition-semitones -9))
     (tenor-sax 
      (:staff-name "tenor sax" :lowest-written bf3
       :highest-written fs6 :staff-short-name "ten sax"       
       :starting-clef treble :chords nil :midi-program 67
       :microtones t :missing-notes (gqs4 gqs5) :clefs-in-c '(treble bass)
       :transposition-semitones -14))
     (baritone-sax 
      (:staff-name "baritone sax" :lowest-written bf3 
       :staff-short-name "bar sax"
       :highest-written fs6 :starting-clef treble :chords nil
       :clefs-in-c '(treble bass)
       :midi-program 68 :microtones t :missing-notes (gqs4 gqs5)
       :transposition-semitones -21))
     (bassoon 
      (:staff-name "bassoon" :lowest-written bf1 :staff-short-name "bsn"
       ;; of course it can go higher but best not to algorithmically select 
       ;; these
       :highest-written c5 :starting-clef bass :chords nil
       :largest-fast-leap 13 :clefs (bass tenor) 
       ;; Wolfgang Ruediger says all 1/4 tones are OK above low E
       :missing-notes (bqf1 bqs1 cqs2 dqf2 dqs2 eqf2)
       :midi-program 71 :microtones t))
     (french-horn
      (:staff-name "french horn" :lowest-written c3 :highest-written c6
       :chords nil :staff-short-name "hn" :midi-program 61 :starting-clef treble
       :transposition-semitones -7 :clefs (treble bass) :microtones t))
     (c-trumpet
      (:staff-name "trumpet in C" :lowest-written fs3 :highest-written c6
       :chords nil :staff-short-name "c tpt" :midi-program 57
       :starting-clef treble :clefs (treble) :microtones t))
     (marimba 
      (:staff-name "marimba" :lowest-written c3 :highest-written c7 :chords t
       :staff-short-name "mba"
       :midi-program 13 :starting-clef treble :clefs (treble) ; (treble bass)
       :microtones nil))
     (vibraphone (:staff-name "vibraphone" :lowest-written f3
                  :staff-short-name "vib"
                  :highest-written f6 :chords t :midi-program 12
                  :starting-clef treble :microtones nil))
     (violin 
      (:staff-name "violin" :lowest-written g3 :highest-written c7 
       :staff-short-name "vln"
       :starting-clef treble :chords t :midi-program 41 :microtones t
       :chord-function violin-chord-selection-fun :largest-fast-leap 13))
     (viola (:staff-name "viola" :lowest-written c3 :starting-clef alto 
             :staff-short-name "vla"
             :highest-written f6 :chords t :midi-program 42 :microtones t
             :chord-function viola-chord-selection-fun :largest-fast-leap 13
             :clefs (alto treble)))
     (viola-d-amore (:staff-name "viola d'amore" :lowest-written a2
                     :staff-short-name "vla d'am"
                     :starting-clef alto :midi-program 41 :microtones t
                     :chord-function nil :largest-fast-leap 13
                     :highest-written f7 :chords t :clefs (alto treble)))
     (cello 
      (:staff-name "cello" :lowest-written c2 :starting-clef bass
       :staff-short-name "vc"
       ;; of course it can go higher but best not to algorithmically select 
       ;; these
       :clefs (bass tenor treble) :highest-written a5 :chords t :midi-program 43
       :microtones t :chord-function cello-chord-selection-fun 
       :largest-fast-leap 12))
     (double-bass 
      (:staff-name "double bass" :lowest-written e2 :starting-clef bass
       :staff-short-name "db"
       :clefs (bass tenor treble) :prefers-notes low
       :highest-written g5 :chords nil :midi-program 44
       :transposition-semitones -12 :microtones t :largest-fast-leap 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun string-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                   instrument set string-III)
  (let* ((diad-down (best-string-diad 
                     ;; have to reverse so that pitch at index is our starting
                     ;; point  
                     (reverse (subseq pitch-list 0 (1+ index)))))
         (diad-up (unless diad-down
                    (best-string-diad (subseq pitch-list index))))
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

(let ((vln-III (make-pitch 'd4)))
  (defun violin-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                     instrument set) 
    (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                                instrument set vln-III)))

(let ((vla-III (make-pitch 'g3)))
  (defun viola-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                    instrument set) 
    (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                                instrument set vla-III)))

(let ((vc-III (make-pitch 'g2)))
  (defun cello-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                    instrument set) 
    (string-chord-selection-fun curve-num index pitch-list pitch-seq 
                                instrument set vc-III)))

(defun piano-chord-fun (curve-num index pitch-list pitch-seq instrument
                        set) 
  (declare (ignore set instrument pitch-seq curve-num))
  (let* ((start (max 0 (- index 3))) ;; try and get 4 notes
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

;; returns the index into the pitch-list of the first note playable on an
;; open-string (pitch object) and the fret number of the first playable note
(defun guitar-chord-get-first-note (open-string pitch-list)
  (loop for p in pitch-list and i from 0 do
       (when (pitch>= p open-string)
         (return (values i (round (midi- p open-string)))))))

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

;;; NB this assumes the pitch-list contains notes that are already playable as
;;; a guitar chord (which was fine in cheat sheet, as that was the premise of
;;; our harmony).
(let ((last-chord '()))
  (defun guitar-chord-selection-fun (curve-num index pitch-list pitch-seq 
                                     instrument set) 
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
      (add-cmn-mark chord (when show-fingering
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
