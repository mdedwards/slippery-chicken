;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             template.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          A skeleton make-slippery-chicken call: use to add data
;;;                   for a new piece.
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    28th January 2013
;;;
;;; $$ Last modified:  18:06:26 Mon Jun  8 2020 CEST
;;;
;;; SVN ID: $Id: template.lsp 5453 2016-01-18 11:58:00Z medward2 $
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
(in-package :sc)
(in-scale :quarter-tone)

;;; Detailed information on this function can be found at
;;; http://tinyurl.com/bc8zagx Alternatively, if in Emacs you place your cursor
;;; over any function name and type Esc-. you should jump to the definition of
;;; the function in the source code (after perhaps being prompted in the
;;; minibuffer for the TAGS file). There you'll find examples and complete
;;; descriptions of all the keyword arguments. More information on EMACS tags
;;; at http://michael-edwards.org/sc/robodoc.html#emacs-tags

(make-slippery-chicken  
 '+your-title-here+ 
 :title "Your Title Here" 
 :composer "Your Name Here"
 ;; +slippery-chicken-standard-instrument-palette+ is the default instrument
 ;; palette. Only specify this slot if you need something special. See
 ;; instruments.lsp in the src directory (or http://tinyurl.com/pjbgnae) for
 ;; details of all pre-defined instruments. But here's an example of the kind
 ;; of thing to do if you do want to define your own.
 :instrument-palette
 '((piccolo (:transposition-semitones 12 :lowest-written d4
             :highest-written c6))
   ;; a more complete definition, by way of example
   (flute (:staff-name "flute" :staff-short-name "fl" 
           :lowest-written c4 :highest-written d7 
           :missing-notes (cqs4 dqf4) 
           :largest-fast-leap 19
           :starting-clef treble
           :chords nil 
           :microtones t 
           :midi-program 74))
   (b-flat-clarinet (:transposition-semitones -2 :lowest-written e3
                 :highest-written c6))  
   (violin (:lowest-written g3 :highest-written c7 :chords t)))
 :ensemble '(((flt ((flute piccolo) :midi-channel 1))
              (clr (b-flat-clarinet :midi-channel 2))
              (vln-one (violin :midi-channel 3 :staff-names "violin 1"))))
 :staff-groupings '(2 1)
 ;; this can contain bar references or bar numbers and looks something like
 ;; '((1 (q 160)) ((2 2 2) 96)) (200 (q 120 "meno mosso"))) where 1 is the bar,
 ;; q is the beat and 160 is the bpm. The (2 2 2) is a reference to a bar of
 ;; the form (section-number sequence-number bar-number). References are
 ;; converted to bar numbers before being stored in the map.
 :tempo-map '((1 (q 60)))
 :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
 :set-map '((1 (set1 set1 set2))
            (2 (set2 set2 set1 set2)))
 :transposition-curve '(0 0 100 0)
 ;; NB all pitches are sounding pitches
 :set-limits-high '((vla (0 b4 100 b4)))
 ;; NB this should be called players-hierarchy but for historical reasons it's
 ;; not i.e. these are player names, not instrument names.
 :instruments-hierarchy '(vln-one flt clr)
 ;; this is a list of bars where rehearsal letters will be set or a list of
 ;; (bar-nmber letter) pairs if you want to set the letter (or other moniker)
 ;; by hand.
 :rehearsal-letters '(10)
 :avoid-melodic-octaves nil
 :snd-output-dir "/tmp"
 :sndfile-palette `(((source-sndfile-grp-1
                        ((test-sndfile-2.aiff :start 0.000 :end 0.504)
                         (test-sndfile-2.aiff :start 0.504 :end 0.884)
                         (test-sndfile-2.aiff :start 0.884 :end 1.608)))
                       (source-sndfile-grp-2
                        ((test-sndfile-3.aiff 
                          :start 0.035 :end 0.426 :frequency 664)
                         (test-sndfile-3.aiff 
                          :start 0.426 :end 0.682 :frequency 664)
                         (test-sndfile-3.aiff 
                          :start 0.682 :end 2.200 :frequency 664))))
                    (,(concatenate 'string 
                                    cl-user::+slippery-chicken-home-dir+ 
                                    "doc/manual/resources/")))
 ;; section number then player, the paired lists of sequence number, instrument
 :instrument-change-map '((1 ((flt ((1 flute) (3 piccolo))))))
 :rthm-seq-palette
 '((seq1 ((((4 4) - 16 16 8 - { 5 - 20 10 20 20 - } { 3 3 6 } )   
           ( - s s s s - (s) - s s s - - +e. s - q))
          :pitch-seq-palette (1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
          :marks (mp 1 a 1 s 3 slur 1 3 a 4 slur 4 7 s 9 a 10 slur 16 18)))  
   (seq2 ((((4 4) - e e - - s s s s - (s) s (s) s { 3 - te te te - } )  
           ( - s s s - (s)  { 3 - te te - (te) } q \+8 (e) )) 
          :pitch-seq-palette (11 10 9 11 10 8 7 11 10 6 5 11 10 9 8 7 11) 
          :marks (mf 1 s 1 2 a 3 slur 3 4 a 5 slur 5 6 s 7 s 8 a 9 
                     slur 9 11 a 12 slur 12 14 s 15 16 a 17))))
 :rthm-seq-map
 '((1 ((flt (seq1 seq1 seq2)) 
       (clr (seq1 seq2 seq1)) 
       (vln-one (seq1 seq1 seq2))))
   (2 ((flt (seq1 seq1 seq2 seq1))
       (clr (seq1 seq1 seq2 seq1))
       (vln-one (seq2 seq2 seq2 seq2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; media content generation:

;;; MIDI file
(midi-play +your-title-here+ :midi-file "/tmp/your-title-here.mid")

;;; Lilypond score
(write-lp-data-for-all +your-title-here+ :base-path "/tmp/")

;;; cmn score
;;; #+ notation means only run the next Lisp form if e.g. the CMN package is
;;; available  
#+cmn (cmn-display +your-title-here+ :file "/tmp/your-title-here.eps")

;;; sound file
#+clm (clm-play +your-title-here+ 1 nil 'source-sndfile-grp-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF template.lsp
