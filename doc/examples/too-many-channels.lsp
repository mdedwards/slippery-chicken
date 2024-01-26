;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             too-many-channels.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          An example that has more than 16 instruments so needs two
;;;                   midi files when exporting.  
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    28th January 2013
;;;
;;; $$ Last modified:  12:35:34 Mon Dec 11 2017 CET
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
;;; todo: would different tracks in a single MIDI file take care of this too?
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
 '+too-many-channels+ 
 :title "Too many channels" 
 :composer "Michael & Brian"
 ;; todo: we should have standard ensembles, like string quartet, chamber
 ;; orchestra, ...
 :ensemble '(((flt ((flute piccolo) :midi-channel 1))
              (ob (oboe :midi-channel 2))
              (clr (b-flat-clarinet :midi-channel 3))
              (bsn (bassoon :midi-channel 5))
              (hn-one (french-horn :midi-channel 6))
              (hn-two (french-horn :midi-channel 7))
              (tr (c-trumpet :midi-channel 8))
              (trb (tenor-trombone :midi-channel 9))
              (pno (piano :midi-channel 11))
              (pno-lh (piano-lh :midi-channel 11))
              (vb (vibraphone :midi-channel 12))
              (vln-one (violin :midi-channel 1))
              (vln-two (violin :midi-channel 2))
              (vla (viola :midi-channel 3))
              (vc (cello :midi-channel 4))
              (db (double-bass :midi-channel 5))))
 :staff-groupings '(4 4 2 1 5)
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
 :set-limits-high '((vla (0 b4 100 b4)))
 ;; this is a list of bars where rehearsal letters will be set or a list of
 ;; (bar-nmber letter) pairs if you want to set the letter (or other moniker)
 ;; by hand.
 :rehearsal-letters '(10)
 :avoid-melodic-octaves nil
 :snd-output-dir "/tmp"
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
(midi-play +too-many-channels+ :midi-file "/tmp/too-many-channels1.mid"
           :voices '(flt ob clr bsn hn-one hn-two tr trb pno pno-lh vb))
(midi-play +too-many-channels+ :midi-file "/tmp/too-many-channels2.mid"
           :voices '(vln-one vln-two vla vc db))

;;; Lilypond score
(lp-display +too-many-channels+ :base-path "/tmp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF 
